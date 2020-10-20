package com.github.thestyleofme.driver.core.api.controller.v1;

import com.github.thestyleofme.driver.core.app.service.PrivilegeService;
import com.github.thestyleofme.driver.core.domain.entity.AccountGroup;
import com.github.thestyleofme.driver.core.infra.privilege.Account;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

/**
 * <p>
 * DatasourceController
 * </p>
 *
 * @author isaac 2020/6/30 19:57
 * @since 1.0.0
 */
@RestController("privilegeController.v1")
@RequestMapping("/driver/v1/{organizationId}/privilege")
@Slf4j
public class PrivilegeController {

    private final PrivilegeService privilegeService;

    public PrivilegeController(PrivilegeService privilegeService) {
        this.privilegeService = privilegeService;
    }

    @PostMapping("/add-account")
    public ResponseEntity<?> addAccount(@PathVariable("organizationId") Long tenantId, String datasourceCode,
                                        @RequestBody Account account) {
        return ResponseEntity.ok(privilegeService.addAccount(tenantId, datasourceCode, account));
    }

    @PostMapping("/update-account")
    public ResponseEntity<?> udpateAccount(@PathVariable("organizationId") Long tenantId, String datasourceCode,
                                           @RequestBody AccountGroup account) {
        return ResponseEntity.ok(privilegeService.updateAccount(tenantId, datasourceCode,
                account.getOldA(),
                account.getNewA()));
    }

    @PostMapping("/drop-account")
    public ResponseEntity<?> dropAccount(@PathVariable("organizationId") Long tenantId,
                                         String datasourceCode,
                                         @RequestBody Account account) {
        privilegeService.dropAccount(tenantId, datasourceCode, account);
        return ResponseEntity.ok(account);
    }

    @GetMapping("/query-accounts")
    public ResponseEntity<?> queryAccounts(@PathVariable("organizationId") Long tenantId,
                                           String datasourceCode,
                                           Account account,
                                           @ApiIgnore Pageable pageable) {
        return ResponseEntity.ok(privilegeService.queryAccounts(tenantId, datasourceCode, account, pageable));
    }

    @PostMapping("/grant-privileges")
    public ResponseEntity<?> grantPrivileges(@PathVariable("organizationId") Long tenantId, String datasourceCode,
                                             @RequestBody Account account) {
        return ResponseEntity.ok(privilegeService.grantPrivilege(tenantId, datasourceCode, account));
    }

    @PostMapping("/revoke-privileges")
    public ResponseEntity<?> revokePrivileges(@PathVariable("organizationId") Long tenantId, String datasourceCode,
                                              @RequestBody Account account) {
        return ResponseEntity.ok(privilegeService.dropPrivilege(tenantId, datasourceCode, account));
    }

    @PostMapping("/query-privileges")
    public ResponseEntity<?> queryPrivileges(@PathVariable("organizationId") Long tenantId, String datasourceCode,
                                             @RequestBody Account account) {
        return ResponseEntity.ok(privilegeService.queryPrivileges(tenantId, datasourceCode, account));
    }

}